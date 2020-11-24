package org.smartregister.reveal.repository;

import net.sqlcipher.database.SQLiteDatabase;

import org.fest.assertions.api.Assertions;
import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.reflect.Whitebox;
import org.robolectric.annotation.Config;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.Context;
import org.smartregister.CoreLibrary;
import org.smartregister.repository.DrishtiRepository;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.shadow.SQLiteDatabaseShadow;

import java.util.ArrayList;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Created by samuelgithengi on 11/24/20.
 */
@Config(shadows = {SQLiteDatabaseShadow.class})
public class RevealRepositoryTest extends BaseUnitTest {

    @Mock
    private Context opensrpContext;

    @Mock
    private android.content.Context context;

    @Mock
    private SQLiteDatabase sqLiteDatabase;

    @Captor
    private ArgumentCaptor<String> stringArgumentCaptor;

    private RevealRepository revealRepository;

    @Before
    public void setUp() {

        Context.bindtypes = new ArrayList<>();
        when(opensrpContext.sharedRepositoriesArray()).thenReturn(new DrishtiRepository[0]);
        revealRepository = new RevealRepository(context, opensrpContext);
    }

    @Test
    public void testOnCreateShouldCreateTablesAndIndexes() {
        int version = BuildConfig.DATABASE_VERSION;
        ReflectionHelpers.setStaticField(BuildConfig.class, "DATABASE_VERSION", 1);
        revealRepository.onCreate(sqLiteDatabase);
        verify(sqLiteDatabase, Mockito.times(35)).execSQL(stringArgumentCaptor.capture());
        for (String sql : stringArgumentCaptor.getAllValues()) {
            Assert.assertThat(sql, CoreMatchers.anyOf(CoreMatchers.containsStringIgnoringCase("CREATE TABLE"),
                    CoreMatchers.containsStringIgnoringCase("CREATE VIRTUAL TABLE"),
                    CoreMatchers.containsStringIgnoringCase("CREATE INDEX")));
        }
        ReflectionHelpers.setStaticField(BuildConfig.class, "DATABASE_VERSION", version);
    }
}
